#!/bin/bash
# Run all benchmarks on host (skipping compilation if binary exists)

BASE_DIR="$(cd "$(dirname "$0")" && pwd)"
MATRICES_DIR="$BASE_DIR/Matrices"

# Ensure Matrices exist
if [ ! -d "$MATRICES_DIR" ]; then
    echo "Error: Matrices directory not found at $MATRICES_DIR"
    exit 1
fi

# List of matrix files
MATRIX_FILES="$MATRICES_DIR/1.matrix $MATRICES_DIR/2.matrix $MATRICES_DIR/3.matrix $MATRICES_DIR/4.matrix $MATRICES_DIR/5.matrix"

# Iterate over all algorithms
for algo_dir in "$BASE_DIR/Algorithms"/*; do
    if [ ! -d "$algo_dir" ]; then continue; fi
    algo=$(basename "$algo_dir")
    
    # Iterate over all languages
    for lang_dir in "$algo_dir"/*; do
        if [ ! -d "$lang_dir" ]; then continue; fi
        lang=$(basename "$lang_dir")
        
        # Skip Media/metadata folders
        if [ "$lang" == "Media" ] || [ "$lang" == "metadata.json" ]; then continue; fi
        
        echo "=== Checking $algo/$lang ==="
        
        # Check if runMe.sh exists
        if [ -f "$lang_dir/runMe.sh" ]; then
            echo "Running $algo/$lang..."
            
            # Execute in subshell to preserve CWD
            (
                cd "$lang_dir"
                # Call runMe.sh with matrix arguments
                # We don't need to source common.sh here, runMe.sh does it.
                # common.sh will handle skipping compilation if binary exists.
                ./runMe.sh $MATRIX_FILES
            )
        else
            echo "Skipping $algo/$lang (no runMe.sh)"
        fi
    done
done

echo "Benchmark run complete."
