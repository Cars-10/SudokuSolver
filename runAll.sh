#!/bin/bash
cd "$(dirname "$0")"

# Create logs directory
mkdir -p logs
rm -f logs/*.log

# 1. Setup Matrix Filter (1-5 only)
echo "Setting up Matrix filter (1-5)..."
mkdir -p Matrices_Filtered
rm -f Matrices_Filtered/*
# Link 1.matrix to 5.matrix from the real Matrices dir
# Assuming ../Matrices is where they are relative to AI-2025/Matrices
# But here we are in root. Real matrices are in Matrices/
ln -sf "$(pwd)/Matrices/1.matrix" Matrices_Filtered/
ln -sf "$(pwd)/Matrices/2.matrix" Matrices_Filtered/
ln -sf "$(pwd)/Matrices/3.matrix" Matrices_Filtered/
ln -sf "$(pwd)/Matrices/4.matrix" Matrices_Filtered/
ln -sf "$(pwd)/Matrices/5.matrix" Matrices_Filtered/

# Backup existing symlink/dir
if [ -L "AI-2025/Matrices" ]; then
    rm "AI-2025/Matrices"
elif [ -d "AI-2025/Matrices" ]; then
    mv "AI-2025/Matrices" "AI-2025/Matrices_Backup"
fi

# Point AI-2025/Matrices to our filtered dir
# We need absolute path or relative path. 
# AI-2025/Matrices -> ../Matrices_Filtered
ln -s ../Matrices_Filtered AI-2025/Matrices

echo "Starting benchmark (Ordered by speed, Matrices 1-5)..."

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
    if [ "$solver" = "Racket" ]; then
        echo "Skipping Racket (too slow)"
        continue
    fi
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

# 4. Cleanup / Restore
echo "Restoring Matrices configuration..."
rm "AI-2025/Matrices"
ln -s ../Matrices AI-2025/Matrices
rm -rf Matrices_Filtered

echo "Benchmark complete."
echo "Generating HTML report..."
python3 generate_report.py --save-history
