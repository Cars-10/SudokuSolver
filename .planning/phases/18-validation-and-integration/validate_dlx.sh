#!/bin/bash
# DLX Algorithm Validation Script
# Validates all DLX implementations against reference iteration count (43 for Matrix 1)

REFERENCE_ITERATIONS=43
DLX_BASE_DIR="/Users/vibe/ClaudeCode/SudokuSolver/Algorithms/DLX"

# Header
echo -e "LANGUAGE\tSTATUS\tITERATIONS\tTIME_MS\tMEMORY_KB\tERROR"

# Find all DLX implementation directories
for dir in "$DLX_BASE_DIR"/*/; do
    # Extract language name from directory
    lang=$(basename "$dir")

    # Path to metrics file
    metrics_file="${dir}metrics.json"

    # Check if metrics file exists
    if [ ! -f "$metrics_file" ]; then
        echo -e "${lang}\tMISSING\t-\t-\t-\tNo metrics.json found"
        continue
    fi

    # Try to extract Matrix 1 results using jq if available, fallback to grep
    if command -v jq &> /dev/null; then
        # Use jq for robust JSON parsing
        result=$(jq -r '.[0].results[] | select(.matrix == "1") | "\(.iterations)\t\(.time)\t\(.memory)"' "$metrics_file" 2>/dev/null)

        if [ -z "$result" ]; then
            # Try alternative structure (some metrics might have different format)
            result=$(jq -r '.results[] | select(.matrix == "1") | "\(.iterations)\t\(.time)\t\(.memory)"' "$metrics_file" 2>/dev/null)
        fi

        if [ -z "$result" ]; then
            echo -e "${lang}\tERROR\t-\t-\t-\tNo Matrix 1 result in metrics"
            continue
        fi

        # Parse the result
        iterations=$(echo "$result" | cut -f1)
        time=$(echo "$result" | cut -f2)
        memory=$(echo "$result" | cut -f3)

    else
        # Fallback to grep/sed if jq not available
        # Look for Matrix 1 entry
        iterations=$(grep -A 15 '"matrix": "1"' "$metrics_file" | grep '"iterations"' | head -1 | sed 's/[^0-9]//g')
        time=$(grep -A 15 '"matrix": "1"' "$metrics_file" | grep '"time"' | head -1 | sed 's/.*: *\([0-9.]*\).*/\1/')
        memory=$(grep -A 15 '"matrix": "1"' "$metrics_file" | grep '"memory"' | head -1 | sed 's/[^0-9]//g')

        if [ -z "$iterations" ]; then
            echo -e "${lang}\tERROR\t-\t-\t-\tCould not parse Matrix 1 data"
            continue
        fi
    fi

    # Convert memory from bytes to KB
    if [ -n "$memory" ] && [ "$memory" -gt 0 ]; then
        memory_kb=$((memory / 1024))
    else
        memory_kb=0
    fi

    # Check status
    if [ "$iterations" -eq "$REFERENCE_ITERATIONS" ]; then
        status="OK"
        error="-"
    else
        status="WRONG"
        error="Expected ${REFERENCE_ITERATIONS}, got ${iterations}"
    fi

    # Output result
    echo -e "${lang}\t${status}\t${iterations}\t${time}\t${memory_kb}\t${error}"
done
