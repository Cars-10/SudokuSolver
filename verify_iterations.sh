#!/bin/bash
# verify_iterations.sh
# Runs all solvers on 1.matrix and checks if iterations == 656

MATRIX="../../Matrices/1.matrix"
EXPECTED=656

echo "Verifying solvers against expected iterations: $EXPECTED"
echo "---------------------------------------------------"

# Find all runMe_ai.sh scripts
find Manual -name "runMe_ai.sh" | sort | while read script; do
    solver=$(basename "$(dirname "$script")")
    
    # Run the script on 1.matrix
    # Capture output to find "Solved in Iterations=X"
    # We use a timeout to prevent hangs (using perl as timeout command is missing on mac)
    output=$(perl -e 'alarm shift; exec @ARGV' 30 "$script" "$MATRIX" 2>/dev/null)
    
    # Extract iterations from JSON output
    iterations=$(echo "$output" | grep -o '"iterations": [0-9]*' | head -n 1 | cut -d' ' -f2 | tr -d '\r')
    
    if [ -z "$iterations" ]; then
        iterations="FAIL"
        # echo "DEBUG: Output for $solver was: $output"
    fi
    
    if [ "$iterations" == "$EXPECTED" ]; then
        echo -e "\033[32m[PASS]\033[0m $solver: $iterations"
    else
        echo -e "\033[31m[FAIL]\033[0m $solver: $iterations"
    fi
done
