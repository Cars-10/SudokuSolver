#!/bin/bash
cd "$(dirname "$0")"

# Output file for metrics
METRICS_FILE="metrics.json"
echo "[" > "$METRICS_FILE"

first=true

# Iterate over matrix files
for matrix in ../../Matrices/*.matrix; do
    if [ "$first" = true ]; then
        first=false
    else
        echo "," >> "$METRICS_FILE"
    fi

    matrix_name=$(basename "$matrix")
    
    # Run the solver and capture output and time
    # Using /usr/bin/time -l for Mac to get max RSS
    # We need to capture stdout for iterations and stderr for time/memory
    
    # Create a temporary file for output
    tmp_out=$(mktemp)
    tmp_err=$(mktemp)
    
    # Run command: /usr/bin/time -l ./Sudoku <matrix_file>
    # Note: The C solver takes the filename as an argument, not stdin
    
    # Start timer
    start_time=$(date +%s.%N)
    
    # Execute
    /usr/bin/time -l ./Sudoku "$matrix" > "$tmp_out" 2> "$tmp_err"
    
    # End timer
    end_time=$(date +%s.%N)
    
    # Calculate duration
    # Ensure leading zero for JSON validity
    duration=$(echo "$end_time - $start_time" | bc | awk '{printf "%f", $0}')
    
    # Parse Iterations
    iterations=$(grep "Solved in Iterations=" "$tmp_out" | cut -d'=' -f2)
    if [ -z "$iterations" ]; then
        iterations=0
        status="fail"
    else
        status="pass"
    fi
    
    # Parse Memory (max resident set size)
    # Mac output: "        123456  maximum resident set size"
    memory=$(grep "maximum resident set size" "$tmp_err" | awk '{print $1}')
    if [ -z "$memory" ]; then memory=0; fi
    
    # Parse CPU User/Sys
    # Mac output: "        0.00 real         0.00 user         0.00 sys"
    cpu_user=$(grep "user" "$tmp_err" | awk '{print $3}')
    if [ -z "$cpu_user" ]; then cpu_user=0; fi
    
    cpu_sys=$(grep "sys" "$tmp_err" | awk '{print $5}')
    if [ -z "$cpu_sys" ]; then cpu_sys=0; fi
    
    # Construct JSON object
    # Use python for safe JSON formatting if available, or manual string construction
    # Manual for now to avoid dependency, but be careful
    
    echo "  {" >> "$METRICS_FILE"
    echo "    \"matrix\": \"$matrix_name\"," >> "$METRICS_FILE"
    echo "    \"time\": $duration," >> "$METRICS_FILE"
    echo "    \"iterations\": $iterations," >> "$METRICS_FILE"
    echo "    \"memory\": $memory," >> "$METRICS_FILE"
    echo "    \"cpu_user\": $cpu_user," >> "$METRICS_FILE"
    echo "    \"cpu_sys\": $cpu_sys," >> "$METRICS_FILE"
    echo "    \"status\": \"$status\"" >> "$METRICS_FILE"
    echo -n "  }" >> "$METRICS_FILE"
    
    # Cleanup
    rm "$tmp_out" "$tmp_err"
done

echo "" >> "$METRICS_FILE"
echo "]" >> "$METRICS_FILE"

# Output the metrics file content to stdout so the aggregator can read it
cat "$METRICS_FILE"
