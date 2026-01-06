#!/bin/bash
# runAllBenchmarks.sh
# Runs matrices 1-5 for all languages found in Languages/ directory.

echo "Starting Global Benchmark Run (Matrices 1-5)..."

failures=()
successes=()

# Get list of languages
for dir in Languages/*; do
    if [ -d "$dir" ]; then
        lang=$(basename "$dir")
        
        # Skip non-language directories if any known ones exist
        if [[ "$lang" == "Backups" || "$lang" == "Media" ]]; then
            continue
        fi

        echo "---------------------------------------------------"
        echo "Processing Language: $lang"
        echo "---------------------------------------------------"
        
        # Run benchmark for Matrices 1-5
        # We use runMeGlobal.sh which handles compilation and execution
        if ./runMeGlobal.sh "$lang" "1-5"; then
            echo "SUCCESS: $lang"
            successes+=("$lang")
        else
            echo "ERROR: Failed to run benchmarks for $lang"
            failures+=("$lang")
        fi
    fi
done

echo "==================================================="
echo "Run Complete."
echo "Successful Languages: ${#successes[@]} - ${successes[*]}"
echo "Failed Languages: ${#failures[@]} - ${failures[*]}"
echo "==================================================="
