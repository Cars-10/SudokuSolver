#!/bin/bash
cd "$(dirname "$0")"

# Create logs directory
mkdir -p logs
rm -f logs/*.log

# No matrix filtering - run on ALL matrices in Matrices/

echo "Starting FINAL benchmark (Ordered by speed, ALL Matrices)..."

# 2. Get Sorted List of Solvers
# If get_solver_order.py fails or returns empty, fall back to glob
if [ -f "get_solver_order.py" ]; then
    solvers=$(python3 get_solver_order.py)
else
    solvers=$(ls AI-2025)
fi

# 3. Run Solvers
# Convert newline separated string to array for iteration
echo "$solvers" | while read -r solver; do
    dir="AI-2025/$solver"
    if [ -d "$dir" ] && [ "$solver" != "Matrices" ] && [ "$solver" != "Matrices_Backup" ]; then
        run_script="$dir/RunMe.sh"
        
        if [ -f "AI-2025/$solver/RunMe.sh" ]; then
            echo "Benchmarking $solver..."
            
            # Update report to highlight this solver
            python3 generate_report.py --highlight "$solver"
            
            # Ensure script is executable
            chmod +x "$run_script"
            
            # Use /usr/bin/time -l to capture CPU/Memory metrics (output to stderr, redirected to run.txt)
            /usr/bin/time -l "$run_script" > "$dir/run.txt" 2>&1
            
            # Update report with results
            python3 generate_report.py --highlight "$solver"
            
            echo "Finished $solver"
        fi
    fi
done

echo "Benchmark complete."
echo "Generating HTML report..."
python3 generate_report.py --save-history
