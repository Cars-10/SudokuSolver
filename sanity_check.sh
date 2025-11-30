#!/bin/bash
cd "$(dirname "$0")"

echo "Starting Sanity Check (1.matrix only)..."
echo "----------------------------------------"

# Create a log file
LOG_FILE="sanity_check.log"
echo "Sanity Check Log" > "$LOG_FILE"

# Iterate over all directories in AI-2025
for dir in AI-2025/*; do
    if [ -d "$dir" ] && [ "$(basename "$dir")" != "Matrices" ] && [ "$(basename "$dir")" != "Matrices_Sanity" ]; then
        solver=$(basename "$dir")
        run_script="$dir/RunMe.sh"
        
        if [ -f "$run_script" ]; then
            # Backup RunMe.sh
            cp "$run_script" "$run_script.bak"
            
            # Modify RunMe.sh to use Matrices_Sanity
            # We use | as delimiter to avoid escaping slashes
            sed -i '' 's|\.\./Matrices|\.\./Matrices_Sanity|g' "$run_script"
            
            # Run the solver
            # We capture output to a temp file to check for errors, but also print status
            if (cd "$dir" && ./RunMe.sh > run_sanity.tmp 2>&1); then
                echo "[PASS] $solver"
                echo "[PASS] $solver" >> "$LOG_FILE"
            else
                echo "[FAIL] $solver"
                echo "[FAIL] $solver" >> "$LOG_FILE"
                echo "--- Error Log for $solver ---" >> "$LOG_FILE"
                cat "$dir/run_sanity.tmp" >> "$LOG_FILE"
                echo "-----------------------------" >> "$LOG_FILE"
            fi
            
            # Cleanup temp file
            rm -f "$dir/run_sanity.tmp"
            
            # Restore RunMe.sh
            mv "$run_script.bak" "$run_script"
        fi
    fi
done

echo "----------------------------------------"
echo "Sanity Check Complete. See $LOG_FILE for details."
