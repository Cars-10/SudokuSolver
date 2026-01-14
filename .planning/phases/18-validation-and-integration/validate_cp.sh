#!/bin/bash
# CP Algorithm Validation Script
# Version: 1.0
# Phase: 18-validation-and-integration
# Purpose: Validate all CP implementations against reference iteration count

set -euo pipefail

# Configuration
REFERENCE_ITERATIONS=67
MATRIX="1"
CP_BASE_DIR="/Users/vibe/ClaudeCode/SudokuSolver/Algorithms/CP"

# Output header
echo -e "Language\tStatus\tIterations\tTime_ms\tMemory_KB"

# Process each CP implementation directory
for dir in "$CP_BASE_DIR"/*/; do
    if [ ! -d "$dir" ]; then
        continue
    fi

    # Extract language name from directory path
    lang=$(basename "$dir")

    # Path to metrics file
    metrics_file="${dir}metrics.json"

    # Check if metrics.json exists
    if [ ! -f "$metrics_file" ]; then
        echo -e "${lang}\tNO_METRICS\t-\t-\t-"
        continue
    fi

    # Check if file is readable
    if [ ! -r "$metrics_file" ]; then
        echo -e "${lang}\tNO_METRICS\t-\t-\t-"
        continue
    fi

    # Extract data using jq (with fallback to grep/awk if jq fails)
    if command -v jq &> /dev/null; then
        # Try to extract iteration count for Matrix 1 using jq
        iterations=$(jq -r ".[] | .results[] | select(.matrix == \"$MATRIX\" and .status == \"success\") | .iterations" "$metrics_file" 2>/dev/null | head -n1)
        time_ms=$(jq -r ".[] | .results[] | select(.matrix == \"$MATRIX\" and .status == \"success\") | .time" "$metrics_file" 2>/dev/null | head -n1)
        memory_bytes=$(jq -r ".[] | .results[] | select(.matrix == \"$MATRIX\" and .status == \"success\") | .memory" "$metrics_file" 2>/dev/null | head -n1)
    else
        # Fallback: use grep/awk
        iterations=$(grep -A10 '"matrix": "'"$MATRIX"'"' "$metrics_file" 2>/dev/null | grep '"iterations"' | head -n1 | awk -F': ' '{print $2}' | tr -d ',')
        time_ms=$(grep -A10 '"matrix": "'"$MATRIX"'"' "$metrics_file" 2>/dev/null | grep '"time"' | head -n1 | awk -F': ' '{print $2}' | tr -d ',')
        memory_bytes=$(grep -A10 '"matrix": "'"$MATRIX"'"' "$metrics_file" 2>/dev/null | grep '"memory"' | head -n1 | awk -F': ' '{print $2}' | tr -d ',')
    fi

    # Check if we got valid data
    if [ -z "$iterations" ] || [ "$iterations" == "null" ] || [ "$iterations" == "0" ]; then
        echo -e "${lang}\tMALFORMED\t-\t-\t-"
        continue
    fi

    # Convert memory from bytes to KB
    if [ -n "$memory_bytes" ] && [ "$memory_bytes" != "null" ] && [ "$memory_bytes" != "0" ]; then
        memory_kb=$(awk "BEGIN {printf \"%.0f\", $memory_bytes / 1024}")
    else
        memory_kb="-"
    fi

    # Format time
    if [ -z "$time_ms" ] || [ "$time_ms" == "null" ]; then
        time_ms="-"
    fi

    # Determine status
    if [ "$iterations" == "$REFERENCE_ITERATIONS" ]; then
        status="CORRECT"
    else
        status="WRONG"
    fi

    echo -e "${lang}\t${status}\t${iterations}\t${time_ms}\t${memory_kb}"
done

# Exit successfully
exit 0
